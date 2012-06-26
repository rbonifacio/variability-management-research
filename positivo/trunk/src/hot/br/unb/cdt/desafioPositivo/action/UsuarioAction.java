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
			StatusMessages.instance().addFromResourceBundle("positivo.usuarioAction.solicitacaoCadastro", usuarioDto.getEmail());
			return "home";
		} catch (ExcecaoUsuarioCadastrado e) {
			StatusMessages.instance().addFromResourceBundle("positivo.usuarioAction.emailExistente", usuarioDto.getEmail());
			return null;
		} 
		catch(InvalidStateException e) {
			notificaErrosValidacao(e);
			return null;
		}
		catch (Exception e) {
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(Mensagens.ERRO_GENERICO);
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
			erros.add("positivo.usuarioAction.confirmacao.email.diferente");
		}
		
		return erros;
	}

	public String confirmaSolicitacaoCadastro() {
		if (!validaSenhaConfirmacaoCadastro()) {
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
		}
		catch (Exception e) {
			e.printStackTrace();
			StatusMessages.instance().addFromResourceBundle(Mensagens.ERRO_GENERICO);
			return null;
		}
	}

	private boolean validaSenhaConfirmacaoCadastro() {
		boolean senhaValida = true;

		if (!CriptografiaUtil.verificaSenha(usuarioDto.getSenha())) {
			facesMessages.addFromResourceBundle(FacesMessage.SEVERITY_ERROR,"positivo.usuarioAction.senhaInvalida");
			senhaValida = false;
		}

		if (!usuarioDto.getSenha().equals(usuarioDto.getConfirmacaoSenha())) {
			facesMessages.addFromResourceBundle(FacesMessage.SEVERITY_ERROR,"positivo.usuarioAction.confirmacaoSenhaInvalida");
			senhaValida = false;
		}
		return senhaValida;
	}

	public String autenticar() {
		try {
			if (credentials.getUsername() == null
					|| credentials.getPassword() == null) {
				facesMessages.addFromResourceBundle(FacesMessage.SEVERITY_ERROR,"positivo.usuarioAction.loginAutenticacaoCamposObrigatorios");
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
			facesMessages.addFromResourceBundle(FacesMessage.SEVERITY_INFO,"positivo.usuarioAction.recuperarSenhaSucesso", usuarioDto.getEmail());
			return "home";
		} catch (ExcecaoUsuarioNaoEncontrado e) {
			facesMessages.addFromResourceBundle(FacesMessage.SEVERITY_ERROR,"positivo.usuarioAction.recuperarSenhaEmailInexistente", usuarioDto.getEmail());
			return null;
		} catch (Exception e) {
			e.printStackTrace();
			// que erro � este? ass: Willian----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
			// resposta: ???
			facesMessages.add(FacesMessage.SEVERITY_ERROR,
					e.getLocalizedMessage());
			return null;
		}

	}
	
	public String atualizarDadosUsuario() {
		try {
			facade.atualizarUsuario(usuarioLogado);
			facesMessages.addFromResourceBundle(FacesMessage.SEVERITY_INFO,"positivo.usuarioAction.atualizacaoDadosSucesso");
			return "sumario";
		}
		catch(Exception e) {
			// que erro � este? ass: Willian----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
			// resposta: ???
			facesMessages.add(FacesMessage.SEVERITY_ERROR, e.getLocalizedMessage());
			return null;
		}
	}
	
	public String alterarSenha() {
		try {
			facade.alterarSenha(usuarioLogado, alteraSenhaDTO);
			facesMessages.addFromResourceBundle(FacesMessage.SEVERITY_INFO,"positivo.usuarioAction.atualizacaoSenhaSucesso");
			return "sumario";
		}
		catch(Exception e) {
			// que erro � este? ass: Willian----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
			// resposta: ???
			facesMessages.add(FacesMessage.SEVERITY_ERROR, e.getLocalizedMessage());
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
