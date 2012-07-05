package br.unb.cdt.desafioPositivo.action;

import java.util.ArrayList;
import java.util.List;

import javax.faces.model.SelectItem;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.international.StatusMessage;
import org.jboss.seam.international.StatusMessages;
import org.jboss.seam.security.Credentials;
import org.jboss.seam.security.Identity;

import com.sun.mail.imap.protocol.Status;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.facade.ExcecaoSenhaDiferente;
import br.unb.cdt.desafioPositivo.facade.ExcecaoSenhaInvalida;
import br.unb.cdt.desafioPositivo.facade.ExcecaoUsuarioCadastrado;
import br.unb.cdt.desafioPositivo.facade.ExcecaoUsuarioNaoEncontrado;
import br.unb.cdt.desafioPositivo.mensagens.Mensagens;
import br.unb.cdt.desafioPositivo.model.Estado;
import br.unb.cdt.desafioPositivo.model.Sexo;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.model.dto.AlteraSenhaDTO;
import br.unb.cdt.desafioPositivo.util.criptografia.CriptografiaUtil;
import br.unb.cdt.desafioPositivo.util.email.EmailUtil;

@Name("usuarioAction")
@AutoCreate
public class UsuarioAction {

	private Usuario usuarioDto;

	@In(create=true)
	private EmailUtil emailUtil;
	
	@In(required=false)
	private Usuario usuarioLogado;
	
	@In
	private AlteraSenhaDTO alteraSenhaDTO;
	
	@In
	private DesafioPositivoFacade facade;

	@In
	private Identity identity;

	@In
	private Credentials credentials;

	public UsuarioAction() {
		usuarioDto = new Usuario();
	}

	public SelectItem[] opcoesEstado() {
		SelectItem[] items = new SelectItem[Estado.values().length];

		int i = 0;

		for (Estado e : Estado.values()) {
			items[i] = new SelectItem();

			items[i].setValue(e);
			items[i].setLabel(e.getSigla());
			items[i].setDescription(e.getEstado());

			i++;
		}

		return items;
	}

	public SelectItem[] opcoesSexo() {
		SelectItem[] items = new SelectItem[Sexo.values().length];

		int i = 0;
		for (Sexo s : Sexo.values()) {
			items[i] = new SelectItem();

			items[i].setValue(s);
			items[i].setLabel(s.getDescricao());
			items[i].setDescription(s.getDescricao());

			i++;
		}

		return items;
	}

	/**
	 * Realiza o cadastro do usuario, inicialmente fazend o uma validacao 
	 * dos dados submetidos em usuarioDto.
	 */
	public String cadastro() {
		List<String> erros = validarDadosCadastrais();
		if(! erros.isEmpty()) {
			populaMensagensErro(erros);
			return null;
		}
		
		try {
			facade.adicionarUsuario(usuarioDto);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.SOLICITACAO_CADASTRO, usuarioDto.getEmail());
			return "home";
		} catch (ExcecaoUsuarioCadastrado e) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.EMAIL_EXISTENTE, usuarioDto.getEmail());
			return null;
		} 
		catch (Exception e) {
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.ERRO_GENERICO);
			return null;
		}
		
	}
	
	private void populaMensagensErro(List<String> erros) {
		for(String e : erros) {
			StatusMessages.instance().addFromResourceBundle(e);
		}
	}

	/*
	 * Metodo que verifica se a confirmacao de email 
	 * eh valida.
	 */
	private List<String> validarDadosCadastrais() {
		List<String> erros = new ArrayList<String>();
		if(usuarioDto.getNome() == null || usuarioDto.getNome().equals("")) {
			erros.add("positivo.novoUsuario.nome.obrigatorio");
		}
		
		if(usuarioDto.getSobrenome() == null || usuarioDto.getSobrenome().equals("")) {
			erros.add("positivo.novoUsuario.sobrenome.obrigatorio");
		}
		
		if(usuarioDto.getEmail() == null || usuarioDto.getConfirmacaoEmail() == null) {
			erros.add("positivo.novoUsuario.confirmacao.email.obrigatorio");
		}
		
		if(! (emailUtil.verificaEmailValido(usuarioDto.getEmail()) && usuarioDto.getEmail().equals(usuarioDto.getConfirmacaoEmail()))) {
			erros.add("positivo.novoUsuario.email.confirmacao.invalida");
		}
		return erros;
	}

	/**
	 * Realiza a confirmacao de um cadastro de usuario solicitado.
	 */
	public String confirmaSolicitacaoCadastro() {
		List<String> erros = validarDadosConfirmacao();
		
		if(!erros.isEmpty()) {
			populaMensagensErro(erros);
			return null;
		}
		
		try {
			facade.confirmarSolicitacaoCadstro(usuarioDto);
			StatusMessages.instance().addFromResourceBundle(Mensagens.USUARIO_CONFIRMA_SOLICITACAO_CADASTRO);
			return "home";
		} catch(ExcecaoUsuarioNaoEncontrado e) {
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(Mensagens.USUARIO_NAO_ENCONTRADO);
			return null;
		} catch (Exception e) {
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(Mensagens.ERRO_GENERICO);
			return null;
		}
	}
	
	/*
	 * Valida os dados da confirmacao de cadastro, retornando uma lista 
	 * com os erros identificados; ou uma lista vazia caso nenhum erro 
	 * tenha sido identificado.
	 */
	private List<String> validarDadosConfirmacao() {
		List<String> erros = new ArrayList<String>();
		if(usuarioDto.getEmail() == null || (!emailUtil.verificaEmailValido(usuarioDto.getEmail()))) {
			erros.add("positivo.confirmaSolicitacaoCadastro.email.obrigatorio");
		}
		
		if(usuarioDto.getCodigoConfirmacaoCadastro() == null || usuarioDto.getCodigoConfirmacaoCadastro().equals("")) {
			erros.add("positivo.confirmaSolicitacaoCadastro.codigo.obrigatorio");
		}
		
		if(! (CriptografiaUtil.verificaSenha(usuarioDto.getSenha()) && usuarioDto.getSenha().equals(usuarioDto.getConfirmacaoSenha()))) {
			erros.add("positivo.confirmaSolicitacaoCadastro.senha.obrigatorio");
		}

		return erros;
	}
	
	private void verificaSenhasInformadas(String senha, String confirmacao) throws ExcecaoSenhaInvalida, ExcecaoSenhaDiferente {
		if (!CriptografiaUtil.verificaSenha(senha)) {
			throw new ExcecaoSenhaInvalida();
		}

		if (!senha.equals(confirmacao)) {
			throw new ExcecaoSenhaDiferente();
		}
	}

	/**
	 * Realiza a autenticacao do usuario. As informacoes de autenticacao
	 * (email, senha) estao disponiveis no objeto injetado credentials.
	 */
	public String autenticar() {
		try {
			if (credentials.getUsername() == null
					|| credentials.getPassword() == null) {
				StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.AUTENTICACAO_CAMPOS_OBRIGATORIOS);
				return null;
			}
			
			if (identity.login().equals("loggedIn")) {
				return "sumario";
			}
			else {
				StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.FALHA_AUTENTICACAO);
				return null;
			}
		} catch (Exception e) {
			StatusMessages.instance().clear();
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.FALHA_AUTENTICACAO);
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Metodo que possibilita re recuperacao da senha do usuario. As informacoes 
	 * necessarios sao encapsuladas no bean usuarioDto
	 */
	public String recuperarSenha() {
		try {
			facade.recuperarSenha(usuarioDto);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.RECUPERAR_SENHA_SUCESSO, usuarioDto.getEmail());
			return "home";
		} catch (ExcecaoUsuarioNaoEncontrado e) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.RECUPERAR_SENHA_INEXISTENTE, usuarioDto.getEmail());
			return null;
		} catch (Exception e) {
			StatusMessages.instance().add(StatusMessage.Severity.ERROR, e.getLocalizedMessage());
			e.printStackTrace();
			return null;
		}

	}
	
	/**
	 * Metodo que permite a atualizacao dos dados do usuario. 
	 * As informacoes submetidas na atualizacao ficam encapsulada no 
	 * bean usuarioLogado.
	 */
	public String atualizarDadosUsuario() {
		List<String> erros = validarDadosAtualizacao();
		if(! erros.isEmpty()) {
			populaMensagensErro(erros);
			return null;
		}
		try {
			facade.atualizarUsuario(usuarioLogado);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.ATUALIZAR_DADOS_SUCESSO);
			return "sumario";
		}
		catch(Exception e) {
			StatusMessages.instance().add(StatusMessage.Severity.ERROR, e.getLocalizedMessage());
			return null;
		}
	}
	
	/*
	 * Valida os dados da atualizacao do cadastro do usuario. 
	 * TODO: posteriormente, esse metodo podera ser refatorado, pois parte da validacao eh 
	 * a mesma do cadastro.
	 */
	private List<String> validarDadosAtualizacao() {
		List<String> erros = new ArrayList<String>();
		
		if(usuarioDto.getNome() == null || usuarioDto.getNome().equals("")) {
			erros.add("positivo.novoUsuario.nome.obrigatorio");
		}
		
		if(usuarioDto.getSobrenome() == null || usuarioDto.getSobrenome().equals("")) {
			erros.add("positivo.novoUsuario.sobrenome.obrigatorio");
		}
		
		return erros;
	}

	/**
	 * Metodo que possibilita a alteracao de senha do usuario. As informacoes 
	 * ficam encapsuladas nos beans usuarioLogado e alteraSenhaDTO. 
	 */
	public String alterarSenha() {
		try {
			verificaSenhasInformadas(alteraSenhaDTO.getNovaSenha(), alteraSenhaDTO.getConfirmacaoNovaSenha());
			facade.alterarSenha(usuarioLogado, alteraSenhaDTO);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.ATUALIZAR_SENHA_SUCESSO);
			return "sumario";
		}catch(ExcecaoSenhaInvalida e){
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(Mensagens.SENHA_INVALIDA);
			return null;
		} catch(ExcecaoSenhaDiferente e){
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(Mensagens.SENHA_DIFERENTE);
			return null;
		} catch(Exception e) {
			StatusMessages.instance().add(StatusMessage.Severity.ERROR, e.getLocalizedMessage());
			return null;
		}
	}
	
	public Usuario getUsuarioDto() {
		return usuarioDto;
	}

	public void setUsuarioDto(Usuario usuarioDto) {
		this.usuarioDto = usuarioDto;
	}
}
