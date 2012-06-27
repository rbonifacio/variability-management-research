package br.unb.cdt.desafioPositivo.action;

import java.util.ArrayList;
import java.util.List;

import javax.faces.application.FacesMessage;
import javax.faces.model.SelectItem;

import org.hibernate.validator.InvalidStateException;
import org.hibernate.validator.InvalidValue;
import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.faces.FacesMessages;
import org.jboss.seam.international.StatusMessage;
import org.jboss.seam.international.StatusMessages;
import org.jboss.seam.security.Credentials;
import org.jboss.seam.security.Identity;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.facade.ExcecaoUsuarioCadastrado;
import br.unb.cdt.desafioPositivo.facade.ExcecaoUsuarioNaoEncontrado;
import br.unb.cdt.desafioPositivo.mensagens.Mensagens;
import br.unb.cdt.desafioPositivo.model.Estado;
import br.unb.cdt.desafioPositivo.model.Sexo;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.model.dto.AlteraSenhaDTO;
import br.unb.cdt.desafioPositivo.util.criptografia.CriptografiaUtil;

@Name("usuarioAction")
@AutoCreate
public class UsuarioAction {

	private Usuario usuarioDto;

	@In(required=false)
	private Usuario usuarioLogado;
	
	@In
	private AlteraSenhaDTO alteraSenhaDTO;
	
	@In
	private DesafioPositivoFacade facade;

	@In
	private FacesMessages facesMessages;

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

	@SuppressWarnings("deprecation")
	public String cadastro() {
		// TODO: validar informacoes submetidas.
		// ou usando validadores, ou implementando um metodo para isso.

//		List<String> erros = validaDadosCadastrais();
//		if (erros.size() > 0) {
//			StringBuffer buffer = new StringBuffer();
//
//			for (String e : erros) {
//				StatusMessages.instance().addFromResourceBundle(e);
//			}
//			return null;
//		}

		try {
			facade.adicionarUsuario(usuarioDto);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.SOLICITACAO_CADASTRO, usuarioDto.getEmail());
			return "home";
		} catch (ExcecaoUsuarioCadastrado e) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.EMAIL_EXISTENTE, usuarioDto.getEmail());
			return null;
		} 
		catch(InvalidStateException e) {
			notificaErrosValidacao(e);
			return null;
		}
		catch (Exception e) {
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.ERRO_GENERICO);
			return null;
		}
		
	}

	private void notificaErrosValidacao(InvalidStateException e) {
		for(InvalidValue v: e.getInvalidValues()) {
			StatusMessages.instance().addFromResourceBundle(v.getMessage(), v.getValue());
		}
	}

	private List<String> validaDadosCadastrais() {
		List<String> erros = new ArrayList<String>();
		if (!usuarioDto.getEmail().equals(usuarioDto.getConfirmacaoEmail())) {
			erros.add(Mensagens.CONFIRMACAO_EMAIL_DIFERENTE);
		}
		
		return erros;
	}

	public String confirmaSolicitacaoCadastro() {
		if (!validaSenhaConfirmacaoCadastro()) {
			return null;
		}

		try {
			facade.confirmarSolicitacaoCadstro(usuarioDto);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.USUARIO_CONFIRMA_SOLICITACAO_CADASTRO);
			return "home";
		} catch(ExcecaoUsuarioNaoEncontrado e) {
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.USUARIO_NAO_ENCONTRADO);
			return null;
		}
		catch (Exception e) {
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.ERRO_GENERICO);
			return null;
		}
	}

	private boolean validaSenhaConfirmacaoCadastro() {
		boolean senhaValida = true;

		if (!CriptografiaUtil.verificaSenha(usuarioDto.getSenha())) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.SENHA_INVALIDA);
			senhaValida = false;
		}

		if (!usuarioDto.getSenha().equals(usuarioDto.getConfirmacaoSenha())) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.SENHA_INVALIDA);
			senhaValida = false;
		}
		return senhaValida;
	}

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
			return null;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	public String recuperarSenha() {
		try {
			facade.recuperarSenha(usuarioDto);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.RECUPERAR_SENHA_SUCESSO, usuarioDto.getEmail());
			return "home";
		} catch (ExcecaoUsuarioNaoEncontrado e) {
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.ERROR, Mensagens.RECUPERAR_SENHA_INEXISTENTE, usuarioDto.getEmail());
			return null;
		} catch (Exception e) {
			e.printStackTrace();
			StatusMessages.instance().add(StatusMessage.Severity.ERROR, e.getLocalizedMessage());
			return null;
		}

	}
	
	public String atualizarDadosUsuario() {
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
	
	public String alterarSenha() {
		try {
			facade.alterarSenha(usuarioLogado, alteraSenhaDTO);
			StatusMessages.instance().addFromResourceBundle(StatusMessage.Severity.INFO, Mensagens.ATUALIZAR_SENHA_SUCESSO);
			return "sumario";
		}
		catch(Exception e) {
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
