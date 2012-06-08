package br.unb.cdt.desafioPositivo.action;

import java.util.ArrayList;
import java.util.List;

import javax.faces.application.FacesMessage;
import javax.faces.model.SelectItem;

import org.jboss.seam.annotations.AutoCreate;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.faces.FacesMessages;
import org.jboss.seam.security.Credentials;
import org.jboss.seam.security.Identity;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.facade.ExcecaoUsuarioCadastrado;
import br.unb.cdt.desafioPositivo.facade.ExcecaoUsuarioNaoEncontrado;
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

		List<String> erros = validaDadosCadastrais();
		if (erros.size() > 0) {
			StringBuffer buffer = new StringBuffer();

			for (String e : erros) {
				facesMessages.add(FacesMessage.SEVERITY_ERROR, e);
			}
			return null;
		}

		if (usuarioDto.getEmail().equals(usuarioDto.getConfirmacaoEmail())) {
			try {
				facade.adicionarUsuario(usuarioDto);
				facesMessages.add(FacesMessage.SEVERITY_INFO,
						"Solicitacao de cadastro realizada com sucesso. Um email foi enviado para "
								+ usuarioDto.getEmail());
				return "home";
			} catch (ExcecaoUsuarioCadastrado e) {
				facesMessages.add(
						FacesMessage.SEVERITY_ERROR,
						"Ja existe um usuario com o email "
								+ usuarioDto.getEmail()
								+ " cadastrado na rede Positivo");
				return null;
			} catch (Exception e) {
				e.printStackTrace();
				facesMessages.add(FacesMessage.SEVERITY_ERROR,
						e.getLocalizedMessage());
				return null;
			}
		} else {
			facesMessages.addToControl("confirmacaoSenha",
					"Verifique seu e-mail.");
			usuarioDto.setConfirmacaoSenha(null);
			return null;
		}
	}

	private List<String> validaDadosCadastrais() {
		List<String> erros = new ArrayList<String>();
		if (!usuarioDto.getEmail().equals(usuarioDto.getConfirmacaoEmail())) {
			erros.add("A confirmacao de email tem que ....");
		}
		// TODO: novas validacoes aqui.
		return erros;
	}

	@SuppressWarnings("deprecation")
	public String confirmaSolicitacaoCadastro() {
		if (!validaSenhaConfirmacaoCadastro()) {
			return null;
		}

		try {
			facade.confirmarSolicitacaoCadstro(usuarioDto);

			facesMessages.add(FacesMessage.SEVERITY_INFO, "Confirmacao de cadastro realizada. Proceda com a autenticacao.");
			return "home";
		} catch (Exception e) {
			e.printStackTrace();
			facesMessages.add(FacesMessage.SEVERITY_ERROR, e.getLocalizedMessage());
			return null;
		}
	}

	@SuppressWarnings("deprecation")
	private boolean validaSenhaConfirmacaoCadastro() {
		boolean senhaValida = true;

		if (!CriptografiaUtil.verificaSenha(usuarioDto.getSenha())) {
			facesMessages
					.add(FacesMessage.SEVERITY_INFO,
							"A senha deve conter pelo menos 8 caracteres, sendo formada por pelo menos um digito "
									+ "um caracter minusculo e um caracter maiusculo.");

			senhaValida = false;
		}

		if (!usuarioDto.getSenha().equals(usuarioDto.getConfirmacaoSenha())) {
			facesMessages.add(FacesMessage.SEVERITY_INFO,
					"A confirmacao de senha deve ser ingual a "
							+ "senha informada.");

			senhaValida = false;
		}
		return senhaValida;
	}

	public String autenticar() {
		try {
			if (credentials.getUsername() == null
					|| credentials.getPassword() == null) {
				facesMessages.add(FacesMessage.SEVERITY_INFO,
						"Campos e-mail e senha sao obrigatorios");

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
			facesMessages
					.add(FacesMessage.SEVERITY_INFO,
							"Solicitacao de recuperacao de senha realizada com sucesso. Um e-mail foi enviado para "
									+ usuarioDto.getEmail());
			return "home";
		} catch (ExcecaoUsuarioNaoEncontrado e) {
			facesMessages.add(FacesMessage.SEVERITY_ERROR,
					"O e-mail informado: " + usuarioDto.getEmail()
							+ " nao esta cadastrado na rede Positivo.");
			return null;
		} catch (Exception e) {
			e.printStackTrace();
			facesMessages.add(FacesMessage.SEVERITY_ERROR,
					e.getLocalizedMessage());
			return null;
		}

	}
	
	public String atualizarDadosUsuario() {
		try {
			facade.atualizarUsuario(usuarioLogado);
			facesMessages.add(FacesMessage.SEVERITY_INFO, "Atualizacao dos dados realizada com sucesso");
			return "sumario";
		}
		catch(Exception e) {
			facesMessages.add(FacesMessage.SEVERITY_ERROR, e.getLocalizedMessage());
			return null;
		}
	}
	
	public String alterarSenha() {
		try {
			facade.alterarSenha(usuarioLogado, alteraSenhaDTO);
			facesMessages.add(FacesMessage.SEVERITY_INFO, "Atualizacao da senha realizada com sucesso");
			return "sumario";
		}
		catch(Exception e) {
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
